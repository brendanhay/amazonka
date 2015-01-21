# Suggested:
#   vagrant plugin install vagrant-vbguest
#
# This will take care of installing the virtualbox guest addons on boot.

Vagrant.configure(2) do |config|
  config.vm.box     = 'amazonka-ubuntu-amd64'
  config.vm.box_url = 'https://github.com/kraksoft/vagrant-box-ubuntu/releases/download/14.10/ubuntu-14.10-amd64.box'

  config.vm.provider 'virtualbox' do |vb|
    vb.cpus   = 2
    vb.memory = '4096'
  end
end
